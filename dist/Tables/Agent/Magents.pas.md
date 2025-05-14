<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário para a gestão de agentes, permitindo a visualização e edição de informações relacionadas a agentes, como endereços, contatos, mercados, plafonds e clientes associados. Ele organiza os dados em abas e fornece funcionalidades para manipulação e exibição de informações detalhadas.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TsPageControl`, `TsSplitter`, `TsScrollBox` e frames personalizados (`TFRAMElistAddresses`, `TFRAMElistContacts`, etc.).
  - Integração com fontes de dados (`MasterSource`) para vinculação de dados mestre-detalhe.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e Tipos:**
      - `TsPageControl` com abas para diferentes categorias de dados (endereços, mercados, plafonds, clientes).
      - Frames personalizados para exibição e edição de dados.
      - Botões de ação como "Imprimir" e "Cancelar".
    - **Ações do Formulário e Efeitos:**
      - Impressão do registro atual.
      - Cancelamento de ações.
      - Manipulação de dados mestre-detalhe.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Imprimir o registro atual.
  - Cancelar alterações.
  - Excluir mercados associados a um agente.
  - Alterar o estado de todos os mercados de clientes.

* **Componentes Principais:**
  - `TsPageControl`: Organiza as informações em abas.
  - Frames personalizados (`TFRAMElistAddresses`, `TFRAMElistContacts`, etc.): Exibem e manipulam dados específicos.
  - `TsSplitter`: Permite redimensionar áreas do formulário.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Imprimir": `se botão clicado então executar função de impressão`.
  - Evento `OnClick` do botão "Cancelar": `se botão clicado então cancelar alterações`.
  - Evento `OnClick` do botão "Excluir Mercado": `se botão clicado então excluir mercado selecionado`.
  - Evento `OnChange` de estado: `se estado alterado então atualizar todos os mercados de clientes`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário (`FormCreate`): Configurações iniciais, como definir a aba ativa e inicializar a lista de mercados.
  - Interações do usuário, como cliques em botões, disparam eventos que executam funções específicas.

* **Dados Necessários:**
  - Informações do agente, como endereços, contatos, mercados, plafonds e clientes.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Imprimir": Disponível apenas se houver um registro selecionado.
  - Ação "Excluir Mercado": Disponível apenas se um mercado estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - `AgentMarketsList`: Inicializado como `";"`.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_CreateFormEdit`: Cria e retorna uma instância do formulário.
  - `FormCreate`: Configurações iniciais do formulário.
  - `m_getData`: Configura as fontes de dados para os frames.
  - `m_Validate`: Valida os dados do formulário.
  - `m_AllCustMktStateChange`: Altera o estado de todos os mercados de clientes.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos explícitas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `Global`, `AgentWithBusinessUnitServiceUtils`, entre outras.

* **Componentes Personalizados:**
  - `TFRAMElistAddresses`, `TFRAMElistContacts`, `TFRAMElistMarkets`, `TFRAMElistPlafonds`, `TFRAMEagentsCustomers`.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `AgentMarketsList` (tipo: string, inicializado como `";"`).
  - Outros campos são gerenciados pelos frames personalizados.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```pascal
  procedure TFORMMagents.FormCreate(Sender: TObject);
  begin
    inherited;
    PGCaddress.ActivePageIndex := 0;
    FAgentMarketsList := ';';
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* `FAgentMarketsList`: Inicializado como `";"` para otimização de recursos.
* Uso de `MasterSource` para vinculação de dados mestre-detalhe.

---

## 12. Conclusão:

O código implementa um formulário robusto para a gestão de agentes, com organização em abas e integração de dados mestre-detalhe. No entanto, faltam validações explícitas e mensagens de erro, o que pode limitar a experiência do usuário.

---

## 13. Resumo Curto:

O formulário de gestão de agentes organiza dados em abas e utiliza frames personalizados para exibição e edição. Ele integra dados mestre-detalhe e oferece funcionalidades como impressão e manipulação de mercados.#### **Magents.pas**

```
unit Magents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA, DBClient,
  kneFRCtrlEditSOA, FRagents, knePrivileges, ImgList,
  sSpeedButton, sBitBtn, ToolWin, ComCtrls, acCoolBar, sPanel,
  kneEnterAsTab, sPageControl, ActnList, kneFRGridEditSOA,
  sSplitter, FRlistcontacts, FRlistAddresses, FRlistMarkets, FRlistPlafonds,
  FRagentsCustomers, sScrollBox;

type
  TFORMMagents = class(TFORMkneBaseEdit)
    ACTprint: TAction;
    SPL1: TsSplitter;
    PGCaddress: TsPageControl;
    TSHaddresses: TsTabSheet;
    SPL2: TsSplitter;
    FRAMElistAddresses1: TFRAMElistAddresses;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmarkets: TsTabSheet;
    FRAMElistMarkets1: TFRAMElistMarkets;
    TSHplafonds: TsTabSheet;
    FRAMElistPlafonds1: TFRAMElistPlafonds;
    TSHcustomers: TsTabSheet;
    FRAMEagentsCustomers1: TFRAMEagentsCustomers;
    SCBagent: TsScrollBox;
    FRAMEagents1: TFRAMEagents;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure ACTprintExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure FRAMElistMarkets1BTNdeleteClick(Sender: TObject);
 
  private
    FAgentMarketsList: string;        //JAR #5930  02-06-2010
    { Private declarations }
    class function m_CreateFormEdit(
      const AOwner: TComponent): TFORMkneBaseEdit;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure SetAgentMarketsList(const Value: string);

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override; 

  public
    { Public declarations }
    procedure m_AllCustMktStateChange;

  published
    { published declarations }
    property AgentMarketsList: string read FAgentMarketsList write SetAgentMarketsList;
  end;

var
  FORMMagents: TFORMMagents;

implementation

{$R *.dfm}

uses
  kneUtils, Global,
  //---
  MaddressAndContact, AgentWithBusinessUnitServiceUtils{NAVOPTECH2022-4802},
  //---
  DRentities;

{ TFORMMagents }

class function TFORMMagents.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
   Result := TFORMMagents.Create(Application);
end;

procedure TFORMMagents.FormCreate(Sender: TObject);
begin
  inherited;
  PGCaddress.ActivePageIndex := 0;
  FAgentMarketsList := ';';          //JAR #5930  02-06-2010
end;


procedure TFORMMagents.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMElistAddresses1.MasterSource := lv_MasterFrame.DStable;        // Detail <- Master
  FRAMElistContacts1.MasterSource  := FRAMElistAddresses1.DStable;   // Detail <- Detail(master)
  FRAMElistMarkets1.MasterSource   := lv_MasterFrame.DStable;        // Detail <- Master
  FRAMElistPlafonds1.MasterSource  := lv_MasterFrame.DStable;        // Detail <- Master
  FRAMEagentsCustomers1.MasterSource  := lv_MasterFrame.DStable;     // Detail <- Master
```

#### **Magents.dfm**

```
inherited FORMMagents: TFORMMagents
  Left = 185
  Top = 101
  Width = 1095
  Height = 671
  Caption = 'Agents Management'
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SPL1: TsSplitter [1]
    Left = 0
    Top = 267
    Width = 1079
    Height = 4
    Cursor = crVSplit
    Align = alTop
    SkinData.SkinSection = 'FORM'
  end
  object SCBagent: TsScrollBox [2]
    Left = 0
    Top = 41
    Width = 1079
    Height = 226
    Align = alTop
    TabOrder = 2
    SkinData.SkinSection = 'PANEL_LOW'
    inline FRAMEagents1: TFRAMEagents
      Left = 0
      Top = 0
      Width = 1075
      Height = 222
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBL1: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindMarket.DBE
      end
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindCountry.DBE
      end
      inherited LBLlanguage: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindLanguage.DBE
      end
      inherited LBLLegalNum: TsLabel
        Font.Color = 5059883
      end
      inherited LBLmarket: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindMarket.DBE
      end
      inherited sLabel2: TsLabel
        Font.Color = 5059883
      end
      inherited sLabel5: TsLabel
        Font.Color = 5059883
      end
      inherited sLabel4: TsLabel
        Font.Color = 5059883
      end
      inherited LBLcurrency: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindCurrency.DBE
        Font.Color = 5059883
      end
      inherited Label1: TsLabel
        Font.Color = 5059883
      end
      inherited PNLfooter: TsPanel
        Top = 188
        Width = 1075
      end
      inherited CBOcommMthd: TcxDBImageComboBox
        TabOrder = 18
        Width = 85
      end
      inherited CHKallCustMkt: TcxDBCheckBox
        Width = 241
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            DataSource = FRAMEagents1.DStable
          end
        end
        inherited PNLcode: TPanel
          inherited DBE: TsDBEdit
            DataSource = FRAMEagents1.DStable
          end
        end
      end
      inherited FRAMEfindCurrency: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            DataSource = FRAMEagents1.DStable
          end
```
<!-- tabs:end -->


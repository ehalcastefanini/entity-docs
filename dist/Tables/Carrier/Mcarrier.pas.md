<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário para a gestão de transportadoras (Carriers Management). Ele permite que os usuários visualizem, editem e gerenciem informações relacionadas a transportadoras, incluindo veículos disponíveis, endereços, contatos, tipos de transportadoras e códigos SAP. O objetivo principal é fornecer uma interface centralizada para gerenciar esses dados de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TsPanel`, `TsSplitter`, `TsPageControl`, `TsTabSheet`, e outros para a interface gráfica.
  - Integração com banco de dados utilizando `DBClient` e `TFRAMEBaseEditSOA`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e Tipos:**
      - Botões (`TsBitBtn`): Ações como "SAP Code".
      - Painéis (`TsPanel`): Organização da interface.
      - Abas (`TsPageControl` e `TsTabSheet`): Navegação entre seções como endereços, contatos, tipos de transportadoras, etc.
      - Divisores (`TsSplitter`): Separação visual entre seções.
    - **Ações do Formulário e Efeitos:**
      - Botão "SAP Code": Abre um formulário relacionado ao código SAP.
      - Alteração de abas: Atualiza a exibição de dados conforme a aba selecionada.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Visualizar e editar informações de transportadoras.
  - Gerenciar veículos disponíveis, endereços, contatos e tipos de transportadoras.
  - Abrir um formulário para gerenciar códigos SAP.

* **Componentes Principais:**
  - `FRAMEcarrier1`: Gerencia informações gerais da transportadora.
  - `FRAMEcarrierAvailableVehicle1`: Gerencia veículos disponíveis.
  - `FRAMElistAddresses1`: Gerencia endereços.
  - `FRAMElistContacts1`: Gerencia contatos.
  - `FRAMEcarrierType1`: Gerencia tipos de transportadoras.
  - `FRAMEcarrierMill1`: Gerencia informações relacionadas ao código SAP.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "SAP Code": `if botão clicado then abrir formulário SAP Code`.
  - Evento `OnChange` da aba: `if aba alterada then atualizar exibição de dados`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`m_CreateFormEdit`): Carrega a interface e configurações iniciais.
  2. Carregamento de dados (`m_getData`): Configura as fontes de dados para os frames e inicializa parâmetros de serviço.
  3. Interações do usuário:
     - Clique no botão "SAP Code" chama `m_OpenSapCodeForm`.
     - Alteração de aba chama `PGCaddressChange`.

* **Dados Necessários:**
  - Informações da transportadora, como veículos, endereços, contatos, tipos e códigos SAP.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "SAP Code": Habilitado apenas se houver um código SAP associado.
  - Alteração de aba: Atualiza os dados exibidos.

* **Filtros Disponíveis:**
  - Não especificado no código.

* **Mensagens de Erro:**
  - Não especificado no código.

* **Valores Padrão dos Campos:**
  - Não especificado no código.

* **Validações e Condições dos Campos:**
  - Não especificado no código.

---

## 5. Funções Principais:

* `m_CreateFormEdit`: Cria e inicializa o formulário.
* `m_getData`: Configura as fontes de dados e inicializa parâmetros de serviço.
* `m_OpenSapCodeForm`: Abre o formulário relacionado ao código SAP.

---

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "SAP Code" é habilitado apenas se houver um código SAP associado.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `DRentities`, `DB`, `Global`: Utilizadas para manipulação de dados e lógica de negócios.

* **Componentes Customizados:**
  - `TFRAMEBaseEditSOA`, `TFRAMEcarrier`, `TFRAMEcarrierAvailableVehicle`, etc.: Componentes personalizados para gerenciar diferentes seções do formulário.

---

## 9. Listagem de Campos e Validações:

* Não há validações explícitas ou mapeamento de campos para colunas de banco de dados especificados no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```pascal
  procedure TFORMMcarrier.m_getData;
  begin
    Screen.Cursor := crHourGlass;
    FRAMEcarrierAvailableVehicle1.MasterSource := lv_MasterFrame.DStable;
    FRAMElistAddresses1.MasterSource := lv_MasterFrame.DStable;
    inherited m_getData;
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* `//NAVOPTECH2022-875`: Indica uma modificação ou melhoria específica.
* `//JAR #10870 2011/10/25`: Indica uma alteração histórica no código.

---

## 12. Conclusão:

O código implementa um formulário robusto para a gestão de transportadoras, com suporte a múltiplas seções e integração com banco de dados. No entanto, faltam detalhes sobre validações de campos, mensagens de erro e filtros disponíveis, o que pode limitar sua usabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O código fornece um formulário para gerenciar transportadoras, incluindo veículos, endereços, contatos e códigos SAP. Ele utiliza componentes personalizados e integrações com banco de dados para oferecer uma interface centralizada e eficiente.#### **Mcarrier.pas**

```
unit Mcarrier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA,
  kneFRGridEditSOA, FRcarrier, FRcarrierAvailableVehicle, kneFRCtrlEditSOA,
  DBClient, knePrivileges, ImgList, sSpeedButton, sBitBtn, ToolWin,
  ComCtrls, acCoolBar, sPanel, kneEnterAsTab, sPageControl, ActnList,
  sSplitter, FRlistContacts, FRlistAddresses, FRcarrierMill, FRcarrierType,
  DsapCode{#NAVOPTECH2022-785};

type
  TFORMMcarrier = class(TFORMkneBaseEdit)
    PNLcarrier: TsPanel;
    FRAMEcarrier1: TFRAMEcarrier;
    sSplitter1: TsSplitter;
    PGCaddress: TsPageControl;
    TSHaddresses: TsTabSheet;
    SH1: TsTabSheet;
    FRAMEcarrierAvailableVehicle1: TFRAMEcarrierAvailableVehicle;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmill: TsTabSheet;
    TSHcarrierType: TsTabSheet;
    FRAMEcarrierType1: TFRAMEcarrierType;
    PNLsapCode: TsPanel;
    BTNsapCode: TsBitBtn;
    ACTsapCode: TAction;
    FRAMEcarrierMill1: TFRAMEcarrierMill;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEcarrierAvailableVehicle1BTNaddClick(Sender: TObject);
    procedure ACTsapCodeExecute(Sender: TObject);
    procedure PGCaddressChange(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
  private
    FormDsapCode: TFORMDsapCode;{#NAVOPTECH2022-785}
    procedure m_SetContactsDataSet(sender: TObject);
    procedure m_HasSapCodeOnMill(Sender: TObject; var pv_flag: Boolean);
    procedure m_OpenSapCodeForm(Sender: TObject);
    procedure m_CodeChanged(Sender: TObject);
    { Private declarations }
  protected
    procedure m_getData; override;
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMMcarrier: TFORMMcarrier;

implementation

{$R *.dfm}

uses
  kneUtils, DRentities, DB, Global;

{ TFORMMcarrier }

class function TFORMMcarrier.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMMcarrier.Create(Application);
end;

procedure TFORMMcarrier.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMEcarrierAvailableVehicle1.MasterSource := lv_MasterFrame.DStable;
  FRAMEcarrierType1.MasterSource := lv_MasterFrame.DStable;       //JAR #10870 2011/10/25

  FRAMElistAddresses1.MasterSource := lv_MasterFrame.DStable;    // Detail <- Master
  FRAMElistContacts1.MasterSource := FRAMElistAddresses1.DStable;   // Detail <- Detail(master)


  // # 3467
  FRAMEcarrierMill1.MasterSource    := lv_MasterFrame.DStable;
  FRAMEcarrierMill1.HasSapCode      := m_HasSapCodeOnMill;
  FRAMEcarrierMill1.OpenSapCodeForm := m_OpenSapCodeForm;

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
//  lv_MasterFrame.ServiceParams.MaxRecords := 0;
//  lv_MasterFrame.ServiceParams.Criteria := '';

  inherited m_getData;

  FRAMElistAddresses1.GetDataSet := m_SetContactsDataSet;   // atribui uma referencia para o dadaset dos Contacts
  PGCaddress.ActivePageIndex := 0;

  //NAVOPTECH2022-875 (cmosilva 22-08-2022)
```

#### **Mcarrier.dfm**

```
inherited FORMMcarrier: TFORMMcarrier
  Left = 402
  Top = 227
  Width = 830
  Height = 525
  Caption = 'Carriers Management'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 814
    inherited CLBactions: TsCoolBar
      Width = 814
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 810
        end>
      inherited PNbotoes: TsPanel
        Width = 797
        object PNLsapCode: TsPanel
          Left = 560
          Top = 1
          Width = 81
          Height = 39
          Align = alLeft
          TabOrder = 7
          Visible = False
          SkinData.SkinSection = 'ALPHACOMBOBOX'
          object BTNsapCode: TsBitBtn
            Left = 2
            Top = 2
            Width = 75
            Height = 30
            Action = ACTsapCode
            Caption = 'SAP Code'
            Enabled = False
            TabOrder = 0
            TabStop = False
            NumGlyphs = 2
            SkinData.SkinSection = 'SPEEDBUTTON'
            Images = IMLbuttons
          end
        end
      end
    end
  end
  object PNLcarrier: TsPanel [2]
    Left = 0
    Top = 41
    Width = 814
    Height = 446
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 214
      Width = 812
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEcarrier1: TFRAMEcarrier
      Left = 1
      Top = 1
      Width = 812
      Height = 213
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited Label3: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindLanguage.DBE
      end
      inherited Label4: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindCountry.DBE
      end
      inherited Label5: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindCurrency.DBE
      end
      inherited Label6: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindPayment.DBE
      end
      inherited PNLfooter: TsPanel
        Top = 179
        Width = 812
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 240
```
<!-- tabs:end -->


<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário para gerenciar informações relacionadas a armazéns e plataformas. Ele permite a visualização, edição e manipulação de dados de armazéns, incluindo transportadoras, serviços, destinos, endereços e moinhos associados. O objetivo principal é fornecer uma interface centralizada para gerenciar essas informações de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TsPanel`, `TsSplitter`, `TsPageControl`, e frames personalizados (`TFRAMEwarehouse`, `TFRAMEwarehouseCarrier`, etc.).
  - Banco de dados para persistência de dados, utilizando `TDataSet` e `TClientDataSet`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e Tipos:**
      - Painéis (`TsPanel`) para organização visual.
      - Abas (`TsPageControl` e `TsTabSheet`) para navegação entre diferentes seções.
      - Frames personalizados para exibir e editar informações específicas.
    - **Ações do Formulário e Efeitos:**
      - Botão de impressão para imprimir o registro atual.
      - Validação de campos ao entrar e sair de determinados campos.
      - Manipulação de dados antes de salvar no banco de dados.

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Exibir informações detalhadas de armazéns e plataformas.
  - Editar dados de transportadoras, serviços, destinos, endereços e moinhos associados.
  - Validar campos ao interagir com o formulário.
  - Imprimir o registro atual.

* **Componentes Principais:**
  - `TFRAMEwarehouse`: Gerencia informações gerais do armazém.
  - `TFRAMEwarehouseCarrier`: Gerencia transportadoras associadas.
  - `TFRAMEwarehouseServices`: Gerencia serviços associados.
  - `TFRAMEwarehouseDest`: Gerencia destinos associados.
  - `TFRAMElistAddresses`: Gerencia endereços associados.
  - `TFRAMElistContacts`: Gerencia contatos associados.
  - `TFRAMEwarehouseMills`: Gerencia moinhos associados.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão de impressão: `se botão clicado então imprimir registro atual`.
  - Evento `OnEnter` de um campo: `se campo focado então executar validação`.
  - Evento `OnExit` de um campo: `se campo perder foco então validar campo`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário: Componentes visuais e frames são carregados.
  - Interação do usuário: O usuário pode navegar entre abas, editar campos e acionar botões.
  - Eventos disparados: Validações e manipulações de dados são realizadas com base nas interações do usuário.

* **Dados Necessários:**
  - Código do armazém.
  - Informações de transportadoras, serviços, destinos, endereços e moinhos.

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão de impressão só deve estar habilitado se um registro estiver selecionado.
  - Campos obrigatórios devem ser preenchidos antes de salvar.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um campo contiver um valor fora do esperado.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explicitamente definidos no código.

* **Validação de Campos e Condições:**
  - Validação de campos ao entrar e sair.
  - Campos relacionados a endereços e contatos dependem de chaves mestres específicas.

## 5. Funções Principais:

* `m_CreateFormEdit`: Cria e inicializa o formulário.
* `m_getData`: Carrega os dados do banco de dados para os frames.
* `m_Validate`: Realiza validações antes de salvar os dados.

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços externos no código fornecido.

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "Endereço" é exibido apenas se a entidade mestre for configurada corretamente.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `kneTypes` para utilitários e tipos personalizados.
  - `DB`, `DBClient` para manipulação de dados.

* **Componentes Personalizados:**
  - `TFRAMEwarehouse`, `TFRAMEwarehouseCarrier`, etc., para gerenciar seções específicas do formulário.

## 9. Listagem de Campos e Validações:

* Código do Armazém (tipo: string, obrigatório).
* Transportadoras (tipo: lista, opcional).
* Serviços (tipo: lista, opcional).
* Destinos (tipo: lista, opcional).
* Endereços (tipo: lista, opcional).
* Contatos (tipo: lista, opcional).
* Moinhos (tipo: lista, opcional).

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```pascal
  procedure TFORMMwarehouse.BTprintCurrentRecordClick(Sender: TObject);
  begin
    // Lógica para imprimir o registro atual
  end;
  ```
* **Capturas de Tela:** Não aplicável.

## 11. Comentários Importantes no Código:

* A propriedade `FAddressMasterKeyFields` é usada para definir a ligação entre endereços e entidades mestres.
* O método `m_getData` otimiza o carregamento de dados para os frames.

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar informações de armazéns e plataformas. Ele utiliza frames personalizados para modularidade e reutilização. No entanto, faltam detalhes sobre validações específicas e mensagens de erro, que poderiam ser melhor documentadas.

## 13. Resumo Curto:

Formulário para gerenciar armazéns e plataformas, com suporte a transportadoras, serviços, destinos, endereços e moinhos. Utiliza frames personalizados para modularidade e validações básicas para garantir a integridade dos dados.#### **Mwarehouse.pas**

```
unit Mwarehouse;

interface
                                      
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA, DBClient,
  kneFRGridEditSOA, FRwarehouse, ComCtrls, FRwarehouseCarrier,
  FRwarehouseServices, kneFRCtrlEditSOA, FRwarehouseDest, knePrivileges,
  sPageControl, sSpeedButton, sBitBtn, ToolWin, acCoolBar, sPanel, ImgList,
  kneEnterAsTab, ActnList, sSplitter, FRlistContacts, FRlistAddresses,
  FRwarehouseMills, DB;

type
  TFORMMwarehouse = class(TFORMkneBaseEdit)
    PNLDescription: TPanel;
    PNLwarehouse: TsPanel;
    FRAMEwarehouse1: TFRAMEwarehouse;
    sSplitter1: TsSplitter;
    PGCdetails: TsPageControl;
    TSHcarriers: TsTabSheet;
    FRAMEwarehouseCarrier1: TFRAMEwarehouseCarrier;
    TSHservices: TsTabSheet;
    FRAMEwarehouseServices1: TFRAMEwarehouseServices;
    TSHdestination: TsTabSheet;
    FRAMEwarehouseDest1: TFRAMEwarehouseDest;
    TSHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmills: TsTabSheet;
    FRAMEwarehouseMills1: TFRAMEwarehouseMills;
    procedure FormShow(Sender: TObject);
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEwarehouse1EDTwhseCodeEnter(Sender: TObject);
    procedure FRAMEwarehouse1EDTwhseCodeExit(Sender: TObject);
    procedure FRAMEwarehouseMills1CDStableBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FoldWhse: string;                  //JAR #5863  14-05-2010
    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;

  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    constructor Create(AOwner: TComponent);

  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields;// � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMMwarehouse: TFORMMwarehouse;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes,
  //---
  DRentities;

{ TFORMMwarehouse }


constructor TFORMMwarehouse.Create(AOwner: TComponent);
begin
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar isso a frame Address ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField dela
  // com este valor
  FAddressMasterKeyFields := 'warehouseCode=entityCode;entityType';

  inherited;
end;

class function TFORMMwarehouse.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMMwarehouse.Create(Application);
end;

procedure TFORMMwarehouse.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMEwarehouseCarrier1.MasterSource := lv_MasterFrame.DStable;
```

#### **Mwarehouse.dfm**

```
inherited FORMMwarehouse: TFORMMwarehouse
  Left = 420
  Top = 185
  Height = 535
  Caption = 'Warehouses and Platforms'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PNLDescription: TPanel [1]
    Left = 512
    Top = 304
    Width = 277
    Height = 197
    BevelOuter = bvNone
    TabOrder = 1
  end
  inherited PNLtoolbar: TsPanel
    Width = 789
    inherited CLBactions: TsCoolBar
      Width = 789
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 785
        end>
      inherited PNbotoes: TsPanel
        Width = 772
      end
    end
  end
  object PNLwarehouse: TsPanel [3]
    Left = 0
    Top = 41
    Width = 789
    Height = 460
    Align = alClient
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 208
      Width = 787
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEwarehouse1: TFRAMEwarehouse
      Left = 1
      Top = 1
      Width = 787
      Height = 207
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindCountry.DBE
      end
      inherited Label1: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindLanguage.DBE
      end
      inherited Label2: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindConsignee.DBE
      end
      inherited Label3: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindCarrier.DBE
      end
      inherited PNLfooter: TsPanel
        Top = 173
        Width = 787
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 128
        Width = 787
        inherited GRPstatus: TsGroupBox
          Width = 787
          inherited DBTXTlastUpd: TsDBText
            DataSource = FRAMEwarehouse1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            DataSource = FRAMEwarehouse1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            Width = 97
          end
        end
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 236
          end
```
<!-- tabs:end -->


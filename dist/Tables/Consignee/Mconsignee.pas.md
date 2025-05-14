<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa uma interface para manutenção de consignatários (Consignee Maintenance). Ele permite que os usuários visualizem, editem e gerenciem informações relacionadas a consignatários, como endereços, contatos, tipos de veículos, dispositivos especiais, custos logísticos padrão, entre outros. O objetivo é fornecer uma interface centralizada para gerenciar essas informações de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento da interface gráfica e lógica de negócios.
  - Componentes visuais personalizados como `TsPanel`, `TsSplitter`, `TsBitBtn`, e `TsPageControl`.
  - Uso de frames reutilizáveis como `TFRAMEconsignee`, `TFRAMEconsBook`, e outros para modularidade.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e Tipos:**
      - Botões (`TsBitBtn`): Ações como "Duplicate" e "Print Current Record".
      - Painéis (`TsPanel`): Organização visual.
      - Abas (`TsPageControl` e `TsTabSheet`): Navegação entre diferentes seções de informações.
      - Divisores (`TsSplitter`): Ajuste de layout.
    - **Ações do Formulário e Efeitos:**
      - Botão "Duplicate": Duplica o registro atual.
      - Botão "Print Current Record": Imprime o registro atual.
      - Timer (`TMRstdLogCost`): Atualiza custos logísticos padrão periodicamente.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Visualizar e editar informações de consignatários.
  - Navegar entre diferentes seções de informações (endereços, contatos, custos logísticos, etc.).
  - Duplicar registros existentes.
  - Imprimir registros.

* **Componentes Principais:**
  - `TFRAMEconsignee`: Gerencia informações gerais do consignatário.
  - `TsPageControl` e `TsTabSheet`: Organizam as diferentes seções de informações.
  - Botões e ações como `ACTduplicate` e `BTprintCurrentRecord`.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Duplicate": `if botão duplicar clicado then duplicar registro`.
  - Evento `OnClick` do botão "Print Current Record": `if botão imprimir clicado then imprimir registro atual`.
  - Evento `OnChange` da aba: `if aba alterada then carregar dados da aba selecionada`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização: O formulário é carregado com os componentes visuais e frames.
  - Interações do Usuário:
    - Clicar em botões dispara eventos como duplicar ou imprimir registros.
    - Navegar entre abas carrega os dados correspondentes.
  - Funções:
    - `m_getData` (arquivo: `Mconsignee`): Carrega os dados do consignatário.
    - `m_Validate` (arquivo: `Mconsignee`): Valida os dados antes de salvar.
    - `m_PutData` (arquivo: `Mconsignee`): Salva os dados no banco.

* **Dados Necessários:**
  - Informações do consignatário como endereço, contatos, tipo de veículo, etc.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Duplicate": Habilitado apenas se o usuário tiver permissão (`FcanDuplicate`).
  - Botão "Print Current Record": Habilitado se houver um registro selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Permissão negada" se o usuário tentar duplicar sem permissão.
  - "Registro inválido" se os dados não forem válidos.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Validações específicas não estão definidas no código.

---

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `m_getData`: Carrega os dados do consignatário.
  - `m_Validate`: Valida os dados antes de salvar.
  - `m_PutData`: Salva os dados no banco.
  - `ACTduplicateExecute`: Duplica o registro atual.
  - `BTprintCurrentRecordClick`: Imprime o registro atual.

---

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneCBedit`, `kneFREditSOA`, `kneFRGridEditSOA`: Componentes personalizados para edição e exibição de dados.
  - `sPageControl`, `sBitBtn`, `sSplitter`: Componentes visuais para interface do usuário.

* **Componentes Personalizados:**
  - Frames como `TFRAMEconsignee`, `TFRAMEconsBook`, etc.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - País (tipo: string, obrigatório).
  - Estado (tipo: string, obrigatório).
  - Mercado (tipo: string, obrigatório).
  - Armazém (tipo: string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```pascal
  procedure ACTduplicateExecute(Sender: TObject);
  begin
    if FcanDuplicate then
      DuplicateRecord
    else
      ShowMessage('Permissão negada');
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* `FAddressMasterKeyFields`: Define a ligação com os endereços, utilizados por várias entidades.
* `FcanDuplicate`: Indica se o usuário tem permissão para duplicar registros.

---

## 12. Conclusão:

O código implementa uma interface robusta para manutenção de consignatários, com suporte a múltiplas seções de dados e ações como duplicar e imprimir registros. No entanto, faltam definições explícitas de validações e mensagens de erro detalhadas.

---

## 13. Resumo Curto:

Interface para manutenção de consignatários, permitindo gerenciar informações como endereços, contatos e custos logísticos. Inclui ações como duplicar e imprimir registros, com suporte a permissões e validações básicas.#### **Mconsignee.pas**

```
unit Mconsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, ComCtrls, kneFREditSOA, FRconsignee,
  kneFRGridEditSOA, FRconsVehType, FRconsRpPosit,
  FRconsSplDevice, FRconsDelDay, kneFRCtrlEditSOA, DBClient,
  FRconsBook, knePrivileges, sPageControl, sSpeedButton, sBitBtn, ToolWin,
  acCoolBar, sPanel, ImgList, kneEnterAsTab, ActnList, sSplitter,
  FRlistContacts, FRlistAddresses, FRconsMill, FRdocumentsInformation,
  FRconsStandardLogisticCosts, FRextShipDelCons, FRentityComm;

type
  TFORMMconsignee = class(TFORMkneBaseEdit)
    PNLconsignee: TsPanel;
    FRAMEconsignee1: TFRAMEconsignee;
    sSplitter1: TsSplitter;
    PGCdetails: TsPageControl;
    TSHbooking: TsTabSheet;
    FRAMEconsBook1: TFRAMEconsBook;
    TSHweekDelDay: TsTabSheet;
    FRAMEconsDelDay1: TFRAMEconsDelDay;
    TSHallowVehicle: TsTabSheet;
    FRAMEconsVehType1: TFRAMEconsVehType;
    TSHspecialDevice: TsTabSheet;
    FRAMEconsSplDevice1: TFRAMEconsSplDevice;
    TSHunitsPositioning: TsTabSheet;
    FRAMEconsRpPosit1: TFRAMEconsRpPosit;
    SHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmill: TsTabSheet;
    FRAMEconsMill1: TFRAMEconsMill;
    TSHdocs: TsTabSheet;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    ACTduplicate: TAction;
    TSHstdLogisticCost: TsTabSheet;
    FRAMEconsStandardLogisticCosts1: TFRAMEconsStandardLogisticCosts;
    TMRstdLogCost: TTimer;
    TSHextShipDelCons: TsTabSheet;
    FRAMEextShipDelCons1: TFRAMEextShipDelCons;
    TSHconsComm: TsTabSheet;
    FRAMEentityComm1: TFRAMEentityComm;
    procedure FormShow(Sender: TObject);
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEconsDelDay1BTNaddClick(Sender: TObject);
    procedure ACTduplicateExecute(Sender: TObject);
    procedure BTNewClick(Sender: TObject);
    procedure TMRstdLogCostTimer(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure PGCdetailsChange(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FcanDuplicate: Boolean; //JAR #5354  18-02-2010  Suporte para o Duplicate, guarda se o utilizador tem permiss�o para efectuar o duplicate

    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure DoProtectControl(Sender: TObject;
      const pv_Args: array of const);
    function SetAllControlsState(const pv_Control: TControl;
      const pv_Enabled: Boolean): Boolean;
    procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
    procedure SetFormState(pv_AccessMode: string);
    procedure SetFrameState(pv_Frame: TFRAMEBaseGridEditSOA;
      pv_CanEdit: Boolean);                               overload;
    procedure SetFrameState(pv_Frame: TFRAMEBaseCtrlEditSOA;
  pv_CanEdit: Boolean; pv_Fields : array of TClass);       overload;
    function m_ConsInfoIsSimilar(pv_Frame: TFRAMEconsignee): Boolean;

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;
    procedure m_PutData; override;

  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    constructor Create(AOwner: TComponent);
  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields;// � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMMconsignee: TFORMMconsignee;

implementation

uses  
  kneUtils, Global, kneFRFindEditSOA,
  ConsigneeServiceUtils {NAVOPTECH2022-4055},
  //---
  DRentities;

{$R *.dfm}
```

#### **Mconsignee.dfm**

```
inherited FORMMconsignee: TFORMMconsignee
  Left = 466
  Top = 87
  Width = 975
  Height = 788
  Caption = 'Consignee Maintenance'
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 959
    inherited CLBactions: TsCoolBar
      Width = 959
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 955
        end>
      inherited PNbotoes: TsPanel
        Width = 942
        inherited BTNseparator2: TsSpeedButton
          Left = 640
        end
        inherited PNLprint: TsPanel
          Visible = True
          inherited BTprintCurrentRecord: TsBitBtn
            OnClick = BTprintCurrentRecordClick
          end
        end
        object PNL1: TsPanel
          Left = 552
          Top = 1
          Width = 88
          Height = 39
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 7
          SkinData.SkinSection = 'ALPHACOMBOBOX'
          object BTNaddress: TsBitBtn
            Left = 3
            Top = 2
            Width = 82
            Height = 30
            Action = ACTduplicate
            Caption = 'Duplicate'
            TabOrder = 0
            TabStop = False
            SkinData.SkinSection = 'SPEEDBUTTON'
            ImageIndex = 10
            Images = IMLbuttons
          end
        end
      end
    end
  end
  object PNLconsignee: TsPanel [2]
    Left = 0
    Top = 41
    Width = 959
    Height = 709
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 312
      Width = 957
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEconsignee1: TFRAMEconsignee
      Left = 1
      Top = 1
      Width = 957
      Height = 311
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindCountry.DBE
      end
      inherited LBLstate: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindOrdDest.DBE
      end
      inherited LBLmarket: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindConsMarket.DBE
      end
      inherited LBLwarehouse: TsLabel
```
<!-- tabs:end -->

